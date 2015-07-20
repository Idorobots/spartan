DC = gdc
DFLAGS = -ggdb3 -Wall -Wextra -pedantic
LDLIBS =
SCM = guile
CAT = cat
RM = rm -f

TMP := $(shell mktemp)

VPATH = src:src/compiler:src/bootstrap:src/runtime:src/stdlib

FOOF_RT = foof
FOOF_SCM = foof.scm
FOOF_FOO = foof.foo

BOOTSTRAP_SCM_SRC = bootstrap.scm
RT_SRC = rt.foo
FOOF_SRC = ast.foo utils.foo compiler.foo rename.foo cpc.foo macro-expander.foo
STD_SRC = core.foo qq.foo list.foo

RUNTIME_OBJS = rt.o

all: runtime #bootstrap unimplemented

runtime: $(FOOF_RT)

$(FOOF_RT): $(RUNTIME_OBJS)
	$(DC) $^ $(LDLIBS) -o $@

bootstrap: $(FOOF_SCM)

$(FOOF_FOO): $(RT_SRC) $(STD_SRC) $(FOOF_SRC)
	$(CAT) $^ > $@

$(FOOF_SCM): $(BOOTSTRAP_SCM_SRC) $(FOOF_FOO)
	$(CAT) $^ > $(TMP)
	$(SCM) -l $(TMP) -e main "$(FOOF_FOO)" > $@  

unimplemented: bootstrap
	cat $(TMP) $(FOOF_SCM) > /tmp/unimplemented.scm

%.o: %.d
	$(DC) $(DFLAGS) $^ -c -o $@

.PHONY: clean

clean:
	$(RM) $(FOOF_RT)
	$(RM) $(RUNTIME_OBJS)
	$(RM) $(FOOF_SCM)
	$(RM) $(FOOF_FOO)
