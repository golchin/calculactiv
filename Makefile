src = app/*.hs src/*.hs
compiler = ghc
output = calculactiv
outputdir = out

all: clean
	$(compiler) -o $(output) -outputdir $(outputdir) $(src)

clean:
	rm -rf $(outputdir)
	rm -f $(output)
