import base64
from dataclasses import dataclass
from pathlib import Path
from typing import Self

from ghc_hyperopt.ghc_config import GHCConfig
from ghc_hyperopt.process_info import ProcessError, ProcessInfo
from ghc_hyperopt.utils import get_logger, pretty_print_json

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

    ghc_config: GHCConfig
    """The GHC configuration used for the build."""

    process_info: ProcessInfo
    """Information about the build process."""

    @classmethod
    def do(
        cls: type[Self],
        project_path: Path,
        component_name: str,
        artifact_dir: Path,
        ghc_config: GHCConfig,
    ) -> Self:
        """Build the benchmark executable."""
        build_dir = artifact_dir / base64.urlsafe_b64encode(hash(ghc_config).to_bytes(8, signed=True)).decode("ascii")

        # Error if the build directory already exists
        if build_dir.exists():
            raise CabalBuildError(f"Build directory already exists: {build_dir}")

        # Create the build directory
        build_dir.mkdir(parents=True)

        args = [
            "cabal",
            "build",
            component_name,
            f"--builddir={build_dir.as_posix()}",
            "--jobs=1",
            # Compiler args
            *(f"--ghc-option={opt}" for opt in ghc_config.to_flags()),
        ]

        # Add a copy of the GHC config and the args to the build directory
        _ = (build_dir / "ghc_config.json").write_text(pretty_print_json(ghc_config))
        _ = (build_dir / "args.txt").write_text("\n".join(args))

        # TODO: Write a copy of the output of the process info to the build directory --
        # we can read it and return it while skipping the build if it exists.
        # This would allow us to resume tuning a build without actually needing to rebuild it!

        try:
            process_info = ProcessInfo.do(
                args=[
                    "cabal",
                    "build",
                    component_name,
                    f"--builddir={build_dir.as_posix()}",
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
            build_dir=build_dir,
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
                f"--builddir={self.build_dir.as_posix()}",
            ],
            project_path=self.project_path,
            logger=logger,
        )

        # Return the path to the executable
        return Path(process_info.stdout.strip())
