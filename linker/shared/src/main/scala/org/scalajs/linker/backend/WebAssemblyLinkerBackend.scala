/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.linker.backend

import scala.concurrent.{ExecutionContext, Future}

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import org.scalajs.logging.Logger

import org.scalajs.linker._
import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable._
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID

import org.scalajs.linker.backend.javascript.{ByteArrayWriter, SourceMapWriter}
import org.scalajs.linker.backend.webassembly._

import org.scalajs.linker.backend.wasmemitter.Emitter

final class WebAssemblyLinkerBackend(config: LinkerBackendImpl.Config)
    extends LinkerBackendImpl(config) {

  require(
    coreSpec.moduleKind == ModuleKind.ESModule,
    s"The WebAssembly backend only supports ES modules; was ${coreSpec.moduleKind}."
  )
  require(
    coreSpec.esFeatures.esVersion >= ESVersion.ES2022,
    s"The WebAssembly backend requires ECMAScript 2022 or later."
  )

  require(coreSpec.targetIsWebAssembly,
      s"A WebAssembly backend cannot be used with CoreSpec targeting JavaScript")

  val loaderJSFileName = OutputPatternsImpl.jsFile(config.outputPatterns, "__loader")

  private val fragmentIndex = new SourceMapWriter.Index

  private val emitter: Emitter = {
    val loaderModuleName = OutputPatternsImpl.moduleName(config.outputPatterns, "__loader")
    new Emitter(Emitter.Config(coreSpec, loaderModuleName))
  }

  val symbolRequirements: SymbolRequirement = emitter.symbolRequirements

  override def injectedIRFiles: Seq[IRFile] = emitter.injectedIRFiles

  def emit(moduleSet: ModuleSet, output: OutputDirectory, logger: Logger)(
      implicit ec: ExecutionContext): Future[Report] = {
    val emitterResult = emitter.emit(moduleSet, logger)

    val writerInputs = if (emitterResult.body.isEmpty) {
      Iterator.empty
    } else {
      val loaderInput = OutputWriter.OneFile(
          loaderJSFileName, true, () => ByteBuffer.wrap(emitterResult.loaderContent))

      Iterator.single(loaderInput) ++ emitterResult.body.iterator.flatMap {
        case (moduleID, emitterModule) => moduleOutput(moduleID, emitterModule)
      }
    }

    for {
      _ <- OutputWriter.write(writerInputs, output, config.maxConcurrentWrites,
          skipContentCheck = false)
    } yield {
      LinkerBackendImpl.report(
        moduleSet,
        ModuleKind.ESModule,
        config.outputPatterns,
        madeSourceMap = false // JS file never has a sourcemap, only WASM
      )
    }
  }

  private def moduleOutput(moduleID: ModuleID,
      emitterModule: Emitter.Result.Module): Iterator[OutputWriter.Input] = {

    val wasmModule = emitterModule.wasmModule

    val watFileName = s"${moduleID.id}.wat"
    val wasmFileName = s"${moduleID.id}.wasm"
    val sourceMapFileName = s"$wasmFileName.map"
    val jsFileName = OutputPatternsImpl.jsFile(config.outputPatterns, moduleID.id)

    import OutputWriter.{OneFile, TwoFiles}

    val maybeWat = if (config.prettyPrint) {
      val file = OneFile(watFileName, true,
          () => {
            val textOutput = TextWriter.write(wasmModule)
            val textOutputBytes = textOutput.getBytes(StandardCharsets.UTF_8)
            ByteBuffer.wrap(textOutputBytes)
          })
      Iterator.single(file)
    } else {
      Iterator.empty
    }

    val emitDebugInfo = !config.minify

    val mainInput = if (config.sourceMap) {
      TwoFiles(wasmFileName, sourceMapFileName, true, () => {
        val sourceMapWriter = new ByteArrayWriter

        val wasmFileURI = s"./$wasmFileName"
        val sourceMapURI = s"./$sourceMapFileName"

        val smWriter = new SourceMapWriter(sourceMapWriter, wasmFileURI,
            config.relativizeSourceMapBase, fragmentIndex)
        val binaryOutput = BinaryWriter.writeWithSourceMap(
            wasmModule, emitDebugInfo, smWriter, sourceMapURI)
        smWriter.complete()

        (binaryOutput, sourceMapWriter.toByteBuffer())
      })
    } else {
      OneFile(wasmFileName, true,
          () => BinaryWriter.write(wasmModule, emitDebugInfo))
    }

    val jsFileInput = OneFile(jsFileName, true, () => ByteBuffer.wrap(emitterModule.jsFileContent))

    maybeWat ++ Iterator(mainInput, jsFileInput)
  }
}
