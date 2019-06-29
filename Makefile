BUILDDIR=_build
VPATH=$(BUILDDIR)
OCAMLDIR=$(shell ocamlopt -where)
$(shell mkdir -p $(BUILDDIR) $(BUILDDIR)/stub $(BUILDDIR)/lib $(BUILDDIR)/stub_generator $(BUILDDIR)/test $(BUILDDIR)/generated)
PACKAGES=vyos1x-config,pcre,ctypes.stubs,ctypes.foreign

GENERATOR_FILES=$(BUILDDIR)/lib/bindings.cmx \
                $(BUILDDIR)/stub_generator/generate.cmx

LIBFILES=$(BUILDDIR)/lib/bindings.cmx \
         $(BUILDDIR)/generated/vyosconfig_bindings.cmx  \
         $(BUILDDIR)/lib/apply_bindings.cmx  \
         $(BUILDDIR)/generated/vyosconfig.o

CAML_INIT=$(BUILDDIR)/stub/init.o

# The files that we'll generate
GENERATED=$(BUILDDIR)/generated/vyosconfig.h \
          $(BUILDDIR)/generated/vyosconfig.c \
          $(BUILDDIR)/generated/vyosconfig_bindings.ml

OSTYPE:=$(shell ocamlfind ocamlc -config | awk '/^os_type:/ {print $$2}')
SYSTEM:=$(shell ocamlfind ocamlc -config | awk '/^system:/ {print $$2}')
EXTDLL:=$(shell ocamlfind ocamlc -config | awk '/^ext_dll:/ {print $$2}')
CC:= $(shell ocamlfind ocamlc -config | awk '/^bytecomp_c_compiler/ {for(i=2;i<=NF;i++) printf "%s " ,$$i}')

ifeq ($(OSTYPE),$(filter $(OSTYPE),Win32 Cygwin))
EXTEXE=.exe
else
EXTEXE=
endif

GENERATOR=$(BUILDDIR)/generate$(EXTEXE)

all: sharedlib

sharedlib: $(BUILDDIR)/libvyosconfig$(EXTDLL)


ifeq ($(OSTYPE),$(filter $(OSTYPE),Win32 Cygwin))
$(BUILDDIR)/libvyosconfig$(EXTDLL): $(CAML_INIT) $(LIBFILES)
	ocamlfind opt -o $@ -linkpkg -output-obj -verbose -package $(PACKAGES) $^
else ifeq ($(SYSTEM),$(filter $(SYSTEM),macosx))
$(BUILDDIR)/libvyosconfig$(EXTDLL): $(CAML_INIT) $(LIBFILES)
	ocamlfind opt -o $@ -linkpkg -runtime-variant _pic -verbose -ccopt -dynamiclib -package $(PACKAGES) $^
else
$(BUILDDIR)/libvyosconfig$(EXTDLL): $(CAML_INIT) $(LIBFILES)
	ocamlfind opt -o $@ -linkpkg -output-obj -runtime-variant _pic -verbose -package $(PACKAGES) -ccopt "-Wl,-soname,libvyosconfig.so.0" $^
endif

stubs: $(GENERATED)

$(BUILDDIR)/stub/%.o:
	ocamlc -g -c stub/init.c
	mv init.o $@

$(GENERATED): $(GENERATOR)
	$(GENERATOR) $(BUILDDIR)/generated

$(BUILDDIR)/%.o: %.c
	$(CC) -c -o $@ -fPIC -I $(shell ocamlfind query ctypes) -I $(OCAMLDIR) -I $(OCAMLDIR)/../ctypes $<

$(BUILDDIR)/%.cmx: %.ml
	ocamlfind opt -c -o $@ -I $(BUILDDIR)/generated -I $(BUILDDIR)/lib -package $(PACKAGES) $<

$(GENERATOR): $(GENERATOR_FILES)
	ocamlfind opt -o $@ -I $(BUILDDIR)/lib -linkpkg -package $(PACKAGES) $^

clean:
	rm -rf $(BUILDDIR)
