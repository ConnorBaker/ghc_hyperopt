env:
	micromamba create -f env.yaml -y

run:
	python3 -m ghc_hyperopt

format:
	ruff format --preview

check:
	ruff check --preview --fix

dashboard:
	optuna-dashboard sqlite:///ghc_hyperopt.db \
		--server gunicorn \
		--artifact-dir optuna-dashboard-artifacts

typecheck:
	pyright .