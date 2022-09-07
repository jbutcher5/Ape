SRC = $(wildcard src/*.cpp)
OBJ = ${SRC:.c=.o}
CXX = clang++

%.o: %.cpp
	${CXX} -c -g $< -o $@

ape: ${OBJ}
	${CXX} $^ -o $@

format: ${SRC}
	clang-format $^ -i

clean:
	rm -p ./src/*.o
