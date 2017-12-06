src = app/*.hs src/*.hs
test = test/*.hs src/*.hs
compiler = ghc
output = calculactiv
outputdir = out
testOutput = spec
testOutputdir = spec-out

all: clean
	$(compiler) -o $(output) -outputdir $(outputdir) $(src)

clean:
	rm -rf $(outputdir)
	rm -f $(output)
	rm -rf $(testOutputdir)
	rm -f $(testOutput)

test: clean
	$(compiler) -o $(testOutput) -outputdir $(testOutputdir) $(test)
	./$(testOutput)
