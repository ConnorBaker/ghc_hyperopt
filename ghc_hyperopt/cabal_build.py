import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Self

from ghc_hyperopt.ghc_config import GHCConfig
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

    ghc_config: GHCConfig
    """The GHC configuration used for the build."""

    process_info: ProcessInfo
    """Information about the build process."""

    @classmethod
    def do(
        cls: type[Self],
        project_path: Path,
        component_name: str,
        ghc_config: GHCConfig,
    ) -> Self:
        # File cleanup
        logger.info("Removing existing build files...")
        shutil.rmtree(project_path / "dist-newstyle")
        (project_path / "cabal.project.local").unlink(missing_ok=True)
        (project_path / "cabal.project.local~").unlink(missing_ok=True)
        logger.info("Removed existing build files.")

        try:
            process_info = ProcessInfo.do(
                args=[
                    "cabal",
                    "build",
                    component_name,
                    "--jobs=1",
                    # Compiler args
                    *(f"--ghc-option={opt}" for opt in ghc_config.to_flags()),
                ],
                project_path=project_path,
                logger=logger,
            )
        except ProcessError as e:
            raise CabalBuildError(*e.args)

        # Return the build info
        return cls(
            project_path=project_path,
            component_name=component_name,
            ghc_config=ghc_config,
            process_info=process_info,
        )

    def list_bin(self) -> Path:
        """Return the path to the previously built component."""
        process_info = ProcessInfo.do(
            args=[
                "cabal",
                "list-bin",
                self.component_name,
            ],
            project_path=self.project_path,
            logger=logger,
        )

        # Return the path to the executable
        return Path(process_info.stdout.strip())
