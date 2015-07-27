.PHONY: all gschem clean check

all: schematic

schematic: create-schematic.scm script.scm define.scm
	gschem -s $<

script.scm: parser.scm netlist assignments
	guile $< > $@

generated.sch: save.scm create-schematic.scm script.scm define.scm
	gschem -s $<

check: standard.sch generated.sch
	@echo Сверка с эталоном
	diff $^
	@echo ! ВАЖНО ! Список соединений полученного эталона не проверялся
	@echo ! ВАЖНО ! на совместимость с заданным в файле netlist!
	@echo *SUCCESS*

clean:
	rm -f script.scm generated.sch
	rm -f *.sch~ sym/*.sym~
