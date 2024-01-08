from typing import ClassVar, final

from optuna import Trial

from ghc_hyperopt.ghc.option import GhcOption
from ghc_hyperopt.ghc.option_group import GhcOptionGroup
from ghc_hyperopt.utils import OurBaseModel, SampleFn, suggest_int


@final
class GhcStgLiftLamOptionsOptunaSampler(OurBaseModel):
    """
    Sampler for GHC compilation options related to how STG lambda lifting is performed.
    """

    STG_LIFT_LAMS_NON_REC_ARGS: ClassVar[SampleFn[Trial, int]] = suggest_int(1, 32)
    STG_LIFT_LAMS_REC_ARGS: ClassVar[SampleFn[Trial, int]] = suggest_int(1, 32)


@final
class GhcStgLiftLamOptions(GhcOptionGroup):
    """
    Tune GHC compilation options related to how STG lambda lifting is performed.
    """

    STG_LIFT_LAMS_NON_REC_ARGS: ClassVar[GhcOption[int]] = GhcOption(
        flag="stg-lift-lams-non-rec-args",
        flag_prefix="-f",
        type=int,
        default=5,
        kind="arg",
        description=(
            """
            Create top-level non-recursive functions with at most <n> parameters while performing late lambda lifting.
            The default is 5, the number of available parameter registers on x86_64.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fstg-lift-lams-non-rec-args",
    )
    STG_LIFT_LAMS_REC_ARGS: ClassVar[GhcOption[int]] = GhcOption(
        flag="stg-lift-lams-rec-args",
        flag_prefix="-f",
        type=int,
        default=5,
        kind="arg",
        description=(
            """
            Create top-level recursive functions with at most <n> parameters while performing late lambda lifting. The
            default is 5, the number of available parameter registers on x86_64.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fstg-lift-lams-rec-args",
    )
