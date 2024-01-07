from dataclasses import dataclass

from ghc_hyperopt.ghc.option import GhcOption, GhcOptionGroup
from ghc_hyperopt.utils import suggest_bool, suggest_int


@dataclass(frozen=True, unsafe_hash=True, slots=True, kw_only=True)
class _GhcInlinerOptions(GhcOptionGroup):
    """
    Represents GHC options which control the inliner.
    """

    title = "GHC inliner tuning"
    description = """
        Tune GHC compilation options related to the inliner.

        When a flag is not specified, the corresponding option is not tuned.
        """

    MAX_INLINE_ALLOC_SIZE: GhcOption[int]
    MAX_INLINE_MEMCPY_INSNS: GhcOption[int]
    MAX_INLINE_MEMSET_INSNS: GhcOption[int]

    EXPOSE_ALL_UNFOLDINGS: GhcOption[bool]


GhcInlinerOptions = _GhcInlinerOptions(
    # TODO: Need to be able to encode "This value should be quite a bit smaller than the block size (typically: 4096)."
    MAX_INLINE_ALLOC_SIZE=GhcOption(
        flag="max-inline-alloc-size",
        flag_prefix="-f",
        type=int,
        default=128,
        sample=suggest_int(32, 4096, log=True),
        kind="arg",
        description=(
            """
            Set the maximum size of inline array allocations to n bytes. GHC will allocate non-pinned arrays of
            statically known size in the current nursery block if they're no bigger than n bytes, ignoring GC
            overheap. This value should be quite a bit smaller than the block size (typically: 4096).
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fmax-inline-alloc-size=⟨n⟩",
    ),
    MAX_INLINE_MEMCPY_INSNS=GhcOption(
        flag="max-inline-memcpy-insns",
        flag_prefix="-f",
        type=int,
        default=32,
        sample=suggest_int(8, 4096, log=True),
        kind="arg",
        description="Inline memcpy calls if they would generate no more than ⟨n⟩ pseudo-instructions.",
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fmax-inline-memcpy-insns=⟨n⟩",
    ),
    MAX_INLINE_MEMSET_INSNS=GhcOption(
        flag="max-inline-memset-insns",
        flag_prefix="-f",
        type=int,
        default=32,
        sample=suggest_int(8, 4096, log=True),
        kind="arg",
        description="Inline memset calls if they would generate no more than n pseudo instructions.",
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fmax-inline-memset-insns=⟨n⟩",
    ),
    EXPOSE_ALL_UNFOLDINGS=GhcOption(
        flag="expose-all-unfoldings",
        flag_prefix="-f",
        type=bool,
        default=False,
        sample=suggest_bool,
        kind="boolean",
        description=(
            """
            An experimental flag to expose all unfoldings, even for very large or recursive functions. This allows for
            all functions to be inlined while usually GHC would avoid inlining larger functions.

            NOTE: Also see the -fspecialise-aggressively flag.
            """
        ),
        reference="https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fexpose-all-unfoldings",
    ),
)
