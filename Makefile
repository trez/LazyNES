SOURCE=Main.hs
TARGET=Main
CC=ghc

all:
	$(CC) -O2 --make $(SOURCE) -o $(TARGET)

prof:
	$(CC) -rtsopts --make $(SOURCE) -prof -auto-all -caf-all -fforce-recomp -o $(TARGET)-prof

clean:
	rm -rf *.o *.hi
	rm -f $(TARGET)
	rm -f $(TARGET)-prof.aux \
          $(TARGET)-prof.hp \
          $(TARGET)-prof \
          $(TARGET)-prof.prof \
          $(TARGET).ps

