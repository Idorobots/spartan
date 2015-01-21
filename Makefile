SCM = guile
CAT = cat
RM = rm -f

TMP := $(shell mktemp)

VPATH = src:src/compiler:src/bootstrap:src/runtime:src/stdlib

FOOF_SCM = foof.scm
FOOF_FOO = foof.foo

BOOTSTRAP_SCM_SRC = bootstrap.scm
RT_SRC = rt.foo
FOOF_SRC = ast.foo utils.foo compiler.foo rename.foo cpc.foo macro-expander.foo
STD_SRC = core.foo qq.foo list.foo

all: bootstrap

bootstrap: $(FOOF_SCM)

$(FOOF_FOO): $(RT_SRC) $(STD_SRC) $(FOOF_SRC)
	$(CAT) $^ > $@

$(FOOF_SCM): $(BOOTSTRAP_SCM_SRC) $(FOOF_FOO)
	$(CAT) $^ > $(TMP)
	$(SCM) -l $(TMP) -e main "$(FOOF_FOO)" > $@  

.PHONY: clean

clean:
	$(RM) $(FOOF_SCM)
	$(RM) $(FOOF_FOO)
