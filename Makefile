
NAME = npuzzle
CC = ghc
SH = bash

SRC_DIR = src/

SRC_FILE = main.hs \
						Rand.hs \

SRC = $(addprefix $(SRC_DIR), $(SRC_FILE))

OBJ = $(SRC:.hs=.o)
HI = $(SRC:.hs=.hi)

all : fclean install $(NAME)

install :
	$(SH) cabal.install.sh

$(NAME) :
	$(CC) -o $(NAME) $(SRC)

clean	:
	rm -f $(OBJ) $(HI)

fclean	: clean
	rm -f $(NAME)

re		: fclean all

run	: fclean $(NAME)
	./$(NAME)

.PHONY	: all clean re fclean install run