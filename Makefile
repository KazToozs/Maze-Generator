
SRC	=	cris.ml

OPT	=	ocamlopt

BC	=	ocamlc	

RM	=	rm -f

OBJB	=	$(SRC:.ml=.cmo) $(SRC:.ml=.cmi)

OBJ	=	$(SRC:.ml=.cmi) $(SRC:.ml=.cmx) $(SRC:.ml=.o)

FLAGS	=	-w Aelz -warn-error A

NAME	=	step1


all:	$(NAME)

$(NAME):
	$(OPT) -o $(NAME) $(FLAGS) $(SRC)
	$(BC) -o $(NAME).byte $(FLAGS) $(SRC)

clean:	
	$(RM) $(OBJ)
	$(RM) $(OBJB)

fclean: clean
	$(RM) $(NAME)
	$(RM) $(NAME).byte

re: fclean all
