import subprocess
from dataclasses import dataclass
from typing import Literal, Self

from ghc_hyperopt.version import Version

type Architecture = Literal["x86_64", "aarch64", "ppc64le"]
_Architectures: set[Architecture] = {"x86_64", "aarch64", "ppc64le"}


def to_architecture(architecture: str) -> Architecture:
    if architecture not in _Architectures:
        raise ValueError(f"Architecture {architecture} is not in {_Architectures}")
    else:
        return architecture


type OperatingSystem = Literal["linux", "darwin", "windows"]
_OperatingSystems: set[OperatingSystem] = {"linux", "darwin", "windows"}


def to_operating_system(operating_system: str) -> OperatingSystem:
    if operating_system not in _OperatingSystems:
        raise ValueError(f"Unknown operating system {operating_system}")
    else:
        return operating_system


@dataclass(frozen=True, unsafe_hash=True, slots=True, kw_only=True)
class GhcInfo:
    """Information about GHC and the target platform."""

    version: Version
    """The version of GHC."""

    platform: str
    """The target platform."""

    architecture: Architecture
    """The target architecture."""

    operating_system: OperatingSystem
    """The target operating system."""

    @classmethod
    def get(cls: type[Self]) -> Self:
        """Get information about GHC and the target platform."""

        # Get the GHC version
        version: Version = Version.parse(
            subprocess.check_output(
                ["ghc", "--numeric-version"],
                text=True,
            ).strip()
        )

        # Get the host platform
        # TODO: Assumption that build and host platforms are the same because we are not cross-compiling
        # or providing the ability to produces binaries which we benchmark on a different platform --
        # everything is local.
        platform: str = subprocess.check_output(
            ["ghc", "--print-host-platform"],
            text=True,
        ).strip()

        (_architecture, _, _operating_system) = platform.split("-")

        architecture: Architecture = to_architecture(_architecture)
        operating_system: OperatingSystem = to_operating_system(_operating_system)

        return cls(
            version=version,
            platform=platform,
            architecture=architecture,
            operating_system=operating_system,
        )
