all:
	$(MAKE) dgp && $(MAKE) fit
dgp:
	sbatch 00-run.sh

fit:
	sbatch 02-run.sh

watchdgp:
	watch tail -n40 logs/00-*.out

watchfit:
	watch tail -n40 logs/02-*.out
	
.PHONY: dgp fit
