from dataclasses import dataclass

from ghc_hyperopt.ghc.option import GhcOption, GhcOptionGroup
from ghc_hyperopt.utils import suggest_bool, suggest_int


@dataclass(frozen=True, unsafe_hash=True, slots=True, kw_only=True)
class _GhcSpecialiserOptions(GhcOptionGroup):
    """
    Represents GHC options which control the specialiser.
    """

    title = "GHC specialiser tuning"
    description = """
        Tune GHC compilation options related to the specialiser.

        When a flag is not specified, the corresponding option is not tuned.
        """

    SPEC_CONSTR_KEEN: GhcOption[bool]
    SPEC_CONSTR_COUNT: GhcOption[int]
    SPEC_CONSTR_THRESHOLD: GhcOption[int]

    LATE_SPECIALISE: GhcOption[bool]

    SPECIALISE_AGGRESSIVELY: GhcOption[bool]


GhcSpecialiserOptions = _GhcSpecialiserOptions(
    SPEC_CONSTR_KEEN=GhcOption(
        flag="spec-constr-keen",
        flag_prefix="-f",
        type=bool,
        default=False,
        sample=suggest_bool,
        kind="boolean",
        description=(
            """
            If this flag is on, call-pattern specialisation will specialise a call (f (Just x)) with an explicit
            constructor argument, even if the argument is not scrutinised in the body of the function. This is
            sometimes beneficial; e.g. the argument might be given to some other function that can itself be
            specialised.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fspec-constr-keen",
    ),
    SPEC_CONSTR_COUNT=GhcOption(
        flag="spec-constr-count",
        flag_prefix="-f",
        type=int,
        default=3,
        sample=suggest_int(1, 16),
        kind="arg",
        description=(
            """
            Set the maximum number of specialisations that will be created for any one function by the SpecConstr
            transformation.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fspec-constr-count=⟨n⟩",
    ),
    SPEC_CONSTR_THRESHOLD=GhcOption(
        flag="spec-constr-threshold",
        flag_prefix="-f",
        type=int,
        default=2000,
        sample=suggest_int(100, 100000, log=True),
        kind="arg",
        description="Set the size threshold for the SpecConstr transformation.",
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fspec-constr-threshold=⟨n⟩",
    ),
    LATE_SPECIALISE=GhcOption(
        flag="late-specialise",
        flag_prefix="-f",
        type=bool,
        default=False,
        sample=suggest_bool,
        kind="boolean",
        description=(
            """
            Runs another specialisation pass towards the end of the optimisation pipeline. This can catch
            specialisation opportunities which arose from the previous specialisation pass or other inlining.

            You might want to use this if you are you have a type class method which returns a constrained type. For
            example, a type class where one of the methods implements a traversal.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--flate-specialise",
    ),
    SPECIALISE_AGGRESSIVELY=GhcOption(
        flag="specialise-aggressively",
        flag_prefix="-f",
        type=bool,
        default=False,
        sample=suggest_bool,
        kind="boolean",
        description=(
            """
            By default only type class methods and methods marked INLINABLE or INLINE are specialised. This flag will
            specialise any overloaded function regardless of size if its unfolding is available. This flag is not
            included in any optimisation level as it can massively increase code size. It can be used in conjunction
            with -fexpose-all-unfoldings if you want to ensure all calls are specialised.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fspecialise-aggressively",
    ),
)
