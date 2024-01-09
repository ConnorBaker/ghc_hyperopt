import subprocess
import time
from collections.abc import Sequence
from pathlib import Path
from typing import Self

import psutil

from ghc_hyperopt.utils import OurBaseException, OurBaseModel, get_logger

logger = get_logger(__name__)


class ProcessError(OurBaseException):
    """An error during execution of a process."""

    pass


class ProcessInfo(OurBaseModel):
    """Information about a finished process."""

    args: Sequence[str]
    """The arguments used to start the process."""

    time_total: float
    """The total time taken by the process, in seconds."""

    mem_peak: int
    """The peak memory usage of the process, in bytes."""

    returncode: int
    """The return code from the process."""

    stdout: str
    """The stdout from the process."""

    stderr: str
    """The stderr from the process."""

    @classmethod
    def do(
        cls: type[Self],
        args: Sequence[str],
        project_path: Path,
    ) -> ProcessError | Self:
        """Run a process."""

        # Monitor the process
        logger.info("Running command %s...", " ".join(args))
        start_time: float = time.time()
        proc = psutil.Popen(
            cwd=project_path.as_posix(),
            args=args,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )

        # Collect memory usage
        mem_peak: int = 0
        try:
            while True:
                memory_info = proc.as_dict(attrs=["memory_info"])["memory_info"]
                if memory_info is None:
                    break
                mem_peak = max(memory_info.rss, mem_peak)
                time.sleep(1)
        except psutil.NoSuchProcess:
            pass

        # Program wrap-up
        returncode = proc.wait()
        end_time: float = time.time()

        time_total: float = end_time - start_time
        stdout: str = proc.stdout.read()
        stderr: str = proc.stderr.read()

        if returncode != 0:
            return ProcessError(f"Command {' '.join(args)} failed with return code {returncode}: {stderr}")

        logger.info("Command complete.")

        # Return the build info
        return cls(
            args=args,
            time_total=time_total,
            mem_peak=mem_peak,
            returncode=returncode,
            stdout=stdout,
            stderr=stderr,
        )
