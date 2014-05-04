cabal configure --enable-tests --enable-library-coverage
cabal test
xcopy /E dist\hpc\mix\LinearProgramming-0.1.0.0 dist\hpc\mix\Tests
hpc.exe markup dist\hpc\tix\Tests\Tests.tix --destdir=dist\hpc\html\Tests --hpcdir=dist\hpc\mix\Tests --exclude=Main --exclude=TestsTableau --exclude=TestsAuxiliaryTableau --exclude=TestsLatex --exclude=TestsCompleteExample --exclude=TestsParser --exclude=TestsProblem --exclude=TestsSimplex --exclude=TestsVariables
hpc.exe report dist\hpc\tix\Tests\Tests.tix                               --hpcdir=dist\hpc\mix\Tests --exclude=Main --exclude=TestsTableau --exclude=TestsAuxiliaryTableau --exclude=TestsLatex --exclude=TestsCompleteExample --exclude=TestsParser --exclude=TestsProblem --exclude=TestsSimplex --exclude=TestsVariables
