import shutil
import subprocess
import tempfile
from collections.abc import Sequence
from dataclasses import dataclass
from pathlib import Path
from typing import Self

from ghc_hyperopt.process_info import ProcessError, ProcessInfo
from ghc_hyperopt.utils import get_logger

logger = get_logger(__name__)


class CabalBuildError(ProcessError):
    """An error occurred during the build."""


@dataclass(frozen=True, unsafe_hash=True, slots=True, kw_only=True)
class CabalBuild:
    """Information about the build."""

    project_path: Path
    """The path to the project."""

    component_name: str
    """The name of the component to build."""

    build_dir: Path
    """The path to store the build artifacts."""

    args: Sequence[str]
    """The arguments used to build."""

    process_info: ProcessInfo
    """Information about the build process."""

    @classmethod
    def do(
        cls: type[Self],
        project_path: Path,
        component_name: str,
        artifact_dir: Path,
        flags: Sequence[str],
    ) -> Self:
        """Build the benchmark executable."""
        build_dir = Path(tempfile.mkdtemp(dir=artifact_dir.as_posix()))

        args = [
            "cabal",
            "build",
            component_name,
            f"--builddir={build_dir.as_posix()}",
            "--jobs=1",
            # Compiler args
            *(f"--ghc-option={opt}" for opt in flags),
        ]

        try:
            process_info = ProcessInfo.do(
                args=args,
                project_path=project_path,
                logger=logger,
            )
        except ProcessError as e:
            shutil.rmtree(build_dir.as_posix())
            raise CabalBuildError(*e.args)

        # Return the build info
        return cls(
            project_path=project_path,
            component_name=component_name,
            build_dir=build_dir,
            args=args,
            process_info=process_info,
        )

    def list_bin(self) -> Path:
        """Return the path to the previously built component."""
        return Path(
            subprocess.check_output(
                args=[
                    "cabal",
                    "list-bin",
                    self.component_name,
                    f"--builddir={self.build_dir.as_posix()}",
                ],
                cwd=self.project_path.as_posix(),
                text=True,
            ).strip()
        )
