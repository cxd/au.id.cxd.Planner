# specify list of sourcefiles here
source = Stack.fs \
Queue.fs \
PlanDefinition.fs \
TreeSearch.fs \
GPS.fs
	 
# specify target file here.
output = au.id.cxd.Planner.dll

# search paths

# targets can be
# one of
# exe, winexe, library or module
#
# 
target = --target:library

opts = --tailcalls+

#embed the fsharp runtime.
static = --standalone

# example references
references = -r:System.Data.dll \
-r:System.Xml.dll \
-r:System.Core.dll \
-r:System.Numerics.dll

all:
	mkdir -p ../build
	fsc $(references) $(target) --out:$(output) $(opts) $(source)
	mv $(output) ../build

clean:
	
	rm ../$(output)

run:
	mono $(output)
	