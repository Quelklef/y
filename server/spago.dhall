{ name = "y-server"
, sources = [ "src/**/*.purs", "../shared/src/**/*.purs" ]
, packages = ../shared/packages.dhall
, dependencies = ../shared/dependencies.dhall
}
