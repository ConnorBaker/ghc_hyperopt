from argparse import ArgumentParser
from pathlib import Path

from ghc_hyperopt.ghc.options import add_all_ghc_option_groups_to_parser

# NOTE:
# With one exception, groups are not designed or intended to be nested.  But by inheritance (from _ActionsContainer)
# nesting isn't blocked nor does it raise any errors.
# https://bugs.python.org/msg356004


def get_arg_parser() -> ArgumentParser:
    arg_parser = ArgumentParser(
        description="Tune GHC compilation or RTS options for a project using Optuna.",
    )

    # Main arguments
    _ = arg_parser.add_argument(
        "--project-path",
        type=Path,
        help="The path to the project to benchmark (e.g., FibHaskell).",
        required=True,
    )
    _ = arg_parser.add_argument(
        "--component-name",
        type=str,
        help="The name of the component to benchmark, (e.g., bench:bench-fib).",
        required=True,
    )
    _ = arg_parser.add_argument(
        "--artifact-dir",
        type=Path,
        help="The directory to store the artifacts.",
        required=True,
    )

    # Exclusive groups for GHC tuning or RTS tuning
    tuning_kind_group = arg_parser.add_mutually_exclusive_group(required=True)
    for name in ["ghc", "rts"]:
        _ = tuning_kind_group.add_argument(
            f"--tune-{name}",
            action="store_true",
            help=f"Tune {name.upper()} options.",
        )

    # GHC tuning groups
    _ = arg_parser.add_argument(
        "--tune-ghc-all",
        action="store_true",
        help="Tune all GHC flags.",
    )
    add_all_ghc_option_groups_to_parser(arg_parser)

    return arg_parser
