package ncreep

import zio.ZIO._
import zio.{Ref, Task, UIO, ZIO}

class SecuritySandboxManager(state: Ref.Synchronized[SandboxState]) {
  private def corruptException = ZIO.fail(SecuritySandboxIsCorrupt)

  def open(provider: JobProvider): Task[Unit] = {
    state.updateZIO {
      case SandboxState.Corrupt => corruptException
      case state: SandboxState.Valid => ZIO.succeed(state.openProvider(provider))
    }
  }
  //  *> debug(s"Opened sandbox - ${provider.id}")

  def close(provider: JobProvider) = {
    state.updateZIO {
      case SandboxState.Corrupt => corruptException
      case state: SandboxState.Valid => ZIO.succeed(state.closeProvider(provider))
    }
  }
  //  *> debug(s"Closed sandbox - ${provider.id}")

  def reset: UIO[Unit] = state.set(SandboxState.empty)

  def isProviderOpen(provider: JobProvider): Task[Boolean] =
    state.get.flatMap {
      case SandboxState.Corrupt => corruptException
      case SandboxState.Valid(currentOpen) => ZIO.succeed(currentOpen.contains(provider))
    }

  def isCorrupt: Task[Boolean] = state.get.map {
    case SandboxState.Corrupt => true
    case _: SandboxState.Valid => false
  }
}

object SecuritySandboxManager {
  val start: UIO[SecuritySandboxManager] =
    Ref.Synchronized.make(SandboxState.empty).map { state =>
      new SecuritySandboxManager(state)
    }
}

sealed trait SandboxState extends Product with Serializable {

  import SandboxState._

  def openProvider(provider: JobProvider): SandboxState = this match {
    case Corrupt => Corrupt
    case Valid(currentOpen) =>
      if (currentOpen.contains(provider)) Corrupt
      else Valid(currentOpen + provider)
  }

  def closeProvider(provider: JobProvider): SandboxState = this match {
    case Corrupt => Corrupt
    case Valid(currentOpen) => Valid(currentOpen - provider)
  }
}

object SandboxState {
  case object Corrupt extends SandboxState
  case class Valid(currentOpen: Set[JobProvider]) extends SandboxState

  val empty: SandboxState = Valid(currentOpen = Set.empty)
}
