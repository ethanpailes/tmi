
build:
	stack build

clean:
	stack clean

monitor:
	hdevtools admin -n --start-server
