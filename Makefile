.PHONY: all clean check

all: generated.sch

generated.sch: parser.scm define.scm netlist assignments
	gschem -s $<

check: standard.sch generated.sch
	@echo Сверка с эталоном
	diff $^
	@echo ! ВАЖНО ! Список соединений полученного эталона не проверялся
	@echo ! ВАЖНО ! на совместимость с заданным в файле netlist!
	@echo *SUCCESS*

clean:
	rm -f generated.sch
	rm -f *.sch~ sym/*.sym~
