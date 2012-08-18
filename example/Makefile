Main: Main.hs Application.hs Routes.hs
	ghc $^

Routes.hs: routes
	../dist/build/routeGenerator/routeGenerator -rp -m Application -n 1 $^ > $@
