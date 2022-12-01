package ncreep.bug2

import ncreep.{PrePostJob, SandboxOptimizerHelper, SecuritySandboxManager}

class SecuritySandboxOptimizer(sandboxManager: SecuritySandboxManager) {
  private val helper =
    new SandboxOptimizerHelper[RankedBatch](sandboxManager, _.provider)

  def addPrePost(jobs: List[RankedBatch]): List[PrePostJob[RankedBatch]] =
    helper.addPrePost(jobs)
}
