from dataclasses import dataclass

from ghc_hyperopt.ghc.option import GhcOption, GhcOptionGroup
from ghc_hyperopt.utils import suggest_int


@dataclass(frozen=True, unsafe_hash=True, slots=True, kw_only=True)
class _GhcStgLiftLamOptions(GhcOptionGroup):
    """
    Represents GHC options related to how STG lambda lifting is performed.
    """

    title = "GHC STG lambda lifting tuning"
    description = """
        Tune GHC compilation options related to how STG lambda lifting is performed.

        When a flag is not specified, the corresponding option is not tuned.
        """

    STG_LIFT_LAMS_NON_REC_ARGS: GhcOption[int]
    STG_LIFT_LAMS_REC_ARGS: GhcOption[int]


GhcStgLiftLamOptions = _GhcStgLiftLamOptions(
    STG_LIFT_LAMS_NON_REC_ARGS=GhcOption(
        flag="stg-lift-lams-non-rec-args",
        flag_prefix="-f",
        type=int,
        default=5,
        sample=suggest_int(1, 32),
        kind="arg",
        description=(
            """
            Create top-level non-recursive functions with at most <n> parameters while performing late lambda lifting.
            The default is 5, the number of available parameter registers on x86_64.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fstg-lift-lams-non-rec-args",
    ),
    STG_LIFT_LAMS_REC_ARGS=GhcOption(
        flag="stg-lift-lams-rec-args",
        flag_prefix="-f",
        type=int,
        default=5,
        sample=suggest_int(1, 32),
        kind="arg",
        description=(
            """
            Create top-level recursive functions with at most <n> parameters while performing late lambda lifting. The
            default is 5, the number of available parameter registers on x86_64.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fstg-lift-lams-rec-args",
    ),
)
