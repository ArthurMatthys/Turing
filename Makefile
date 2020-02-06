.PHONY: all clean fclean re

NAME = ft_turing

PKG = yojson

CC = ocamlfind ocamlopt
CC_INT = ocamlc

SIDE_DIR = ./_build/

CFLAGS += -linkpkg -thread -package $(PKG)

# **************************************************************************** #
#                                  SOURCES                                     #
# **************************************************************************** #

OBJ_DIR = $(SIDE_DIR)obj/

SRC_DIR = ./src/

SRC_ = \
	   ft_turing.ml

OBJ = $(SRC_:%.ml=$(OBJ_DIR)%.cmx)

SRC = $(addprefix $(SRC_DIR), $(SRC_))

# **************************************************************************** #
#                                  SOURCES                                     #
# **************************************************************************** #

CMI_DIR = $(SIDE_DIR)cmi/

INT_DIR = ./src/

INT_ = \

CMI = $(INT_:%.mli=$(CMI_DIR)%.cmi)

INT = $(addprefix $(INT_DIR), $(INT_))

# **************************************************************************** #
#                                    RULES                                     #
# **************************************************************************** #


all: $(NAME)

install:
	@brew install opam
	@eval $(opam config env)
	@opam install $(PKG)

$(NAME): $(OBJ_DIR) $(OBJ) $(CMI_DIR) $(CMI)
	@printf "== \x1b[35m$(NAME)\x1b[0m ========================================================\n"
	$(CC) $(CFLAGS) $(OBJ) -o $(NAME)


$(OBJ_DIR):
	@mkdir -p $@

$(CMI_DIR):
	@mkdir -p $@

printsrc:
	@printf "== \x1b[34msources\x1b[0m ========================================================\n"

printint:
	@printf "== \x1b[34minterfaces\x1b[0m =====================================================\n"

$(OBJ_DIR)%.cmx: $(SRC_DIR)%.ml printsrc 
	@printf "$< -> $@\n"
	@$(CC) $(CFLAGS) -c -o $@ $<

$(CMI_DIR)%.cmi: $(INT_DIR)%.mli printint
	@printf "$< -> $@\n"
	@$(CC_INT) -o $< -c $@

clean:
	rm -rf $(SIDE_DIR)

fclean: clean
	rm -rf $(NAME)

re: fclean all
