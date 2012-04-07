# specify list of sourcefiles here
source = BlocksTest.fs \
TestDrive.fs \
TestTreeSearch.fs
	 
# specify target file here.
output = au.id.cxd.Planner.Test.dll

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
-r:System.Numerics.dll \
-r:System.Windows.Forms.dll \
-r:System.Drawing.dll \
-r:../build/au.id.cxd.Planner.dll \
-r:../libs/OpenTK.Compatibility.dll \
-r:../libs/OpenTK.GLControl.dll \
-r:../libs/OpenTK.dll

all:
	mkdir -p ../build
	fsc $(references) $(target) --out:$(output) $(opts) $(source)
	mv $(output) ../build
	cp ../libs/OpenTK.Compatibility.dll ../build
	cp ../libs/OpenTK.GLControl.dll ../build	
	cp ../libs/OpenTK.dll ../build

clean:
	
	rm ../$(output)

run:
	mono $(output)
	