from dataclasses import dataclass

from ghc_hyperopt.ghc.option import GhcOption, GhcOptionGroup
from ghc_hyperopt.utils import suggest_bool


@dataclass(frozen=True, unsafe_hash=True, slots=True, kw_only=True)
class _GhcQuestionableOptions(GhcOptionGroup):
    """
    Represents GHC options which are questionable in some respect.
    """

    title = "⚠️  Questionable GHC tuning ⚠️ "
    description = """
        Tune questionable GHC compilation options. These options are questionable because they have
        historically caused problems or may break things or give bogus benchmark results.

        When a flag is not specified, the corresponding option is not tuned.
        """

    STATIC_ARGUMENT_TRANSFORMATION: GhcOption[bool]
    POLYMORPHIC_SPECIALISATION: GhcOption[bool]
    REGS_GRAPH: GhcOption[bool]
    REGS_ITERATIVE: GhcOption[bool]


GhcQuestionableOptions = _GhcQuestionableOptions(
    # TODO: Tasty-Bench says that this value must always be false, as it is used to allow benchmarking.
    STATIC_ARGUMENT_TRANSFORMATION=GhcOption(
        flag="static-argument-transformation",
        flag_prefix="-f",
        type=bool,
        default=False,
        sample=suggest_bool,
        kind="boolean",
        description=(
            """
            Turn on the static argument transformation, which turns a recursive function into a non-recursive one with
            a local recursive loop. See Chapter 7 of Andre Santos's PhD thesis.

            NOTE: This flag is known to cause problems. See the following quote from the Tasty-Bench documentation:

            > Never compile benchmarks with -fstatic-argument-transformation, because it breaks a trick we use to force
            > GHC into reevaluation of the same function application over and over again.
            > - https://github.com/Bodigrim/tasty-bench#troubleshooting
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fstatic-argument-transformation",
    ),
    POLYMORPHIC_SPECIALISATION=GhcOption(
        flag="polymorphic-specialisation",
        flag_prefix="-f",
        type=bool,
        default=False,
        sample=suggest_bool,
        kind="boolean",
        description=(
            """
            Warning, this feature is highly experimental and may lead to incorrect runtime results. Use at your own
            risk (#23469, #23109, #21229, #23445).

            Enable specialisation of function calls to known dictionaries with free type variables. The created
            specialisation will abstract over the type variables free in the dictionary.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fpolymorphic-specialisation",
    ),
    REGS_GRAPH=GhcOption(
        flag="regs-graph",
        flag_prefix="-f",
        type=bool,
        default=False,
        sample=suggest_bool,
        kind="boolean",
        description=(
            """
            Only applies in combination with the native code generator. Use the graph colouring register allocator for
            register allocation in the native code generator. By default, GHC uses a simpler, faster linear register
            allocator. The downside being that the linear register allocator usually generates worse code.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fregs-graph",
    ),
    # TODO: Must be able to express dependency on REGS_GRAPH.
    REGS_ITERATIVE=GhcOption(
        flag="regs-iterative",
        flag_prefix="-f",
        type=bool,
        default=False,
        sample=suggest_bool,
        kind="boolean",
        description=(
            """
            Only applies in combination with the native code generator. Use the iterative coalescing graph colouring
            register allocator for register allocation in the native code generator. This is the same register
            allocator as the -fregs-graph one but also enables iterative coalescing during register allocation.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fregs-iterative",
    ),
)
