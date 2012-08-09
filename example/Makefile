Main: Main.hs Application.hs Routes.hs
	ghc $^

Routes.hs: routes
	../dist/build/routeGenerator/routeGenerator $^ Application 1 > $@
