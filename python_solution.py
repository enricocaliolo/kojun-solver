def print_board(board):
    for i in range(len(board)):
        print(board[i])
    print("------------------")


def generate_board():
    with open("kojun_10x10.txt", "r") as f:
        n_grid = int(f.readline())

        board = []

        for i in range(n_grid):
            board.append([])
            line = f.readline().split(" ")
            for j in range(n_grid):
                board[i].append(int(line[j]))

        regions = {}
        for i in range(n_grid):
            lines = f.readline().split(" ")
            for j in range(n_grid):
                line = int(lines[j])
                if not line in regions:
                    regions[line] = []

                regions[line].append((i, j))

    return (board, regions)


def find_empty_cell(board):
    for i in range(len(board)):
        for j in range(len(board)):
            if board[i][j] == 0:
                return (i, j)

    return None


def find_region(empty_cell, regions):
    for region, region_cells in regions.items():
        if empty_cell in region_cells:
            return (region, region_cells)


def is_valid(board, regions, empty_cell, num):
    # checar se número existe na região da empty_cell
    # checar ortogonalmente as adjacências
    # checar se há alguma célula acima ou abaixo

    region, region_cells = find_region(empty_cell, regions)

    # checa se número já existe na região
    for cell in regions[region]:
        if board[cell[0]][cell[1]] == num:
            return False

    # checa se número é válido
    if num > len(region_cells):
        return False

    # checando adjacências da célula
    x, y = empty_cell

    # número em cima maior
    for row in range(x):
        region_cell, _ = find_region((row, y), regions)
        if region_cell == region and board[row][y] != 0 and board[row][y] < num:
            return False

    # Check Above
    if 0 <= x - 1 < len(board) and 0 <= y < len(board) and board[x - 1][y] == num:
        return False

    # Check Below
    if 0 <= x + 1 < len(board) and 0 <= y < len(board) and board[x + 1][y] == num:
        return False

    # Check Left
    if 0 <= x < len(board) and 0 <= y - 1 < len(board) and board[x][y - 1] == num:
        return False

    # Check Right
    if 0 <= x < len(board) and 0 <= y + 1 < len(board) and board[x][y + 1] == num:
        return False

    return True


def solve(board, regions):
    empty_cell = find_empty_cell(board)

    if not empty_cell:
        return True

    for num in range(1, len(board) + 1):
        if is_valid(board, regions, empty_cell, num):
            board[empty_cell[0]][empty_cell[1]] = num

            if solve(board, regions):
                return True

            board[empty_cell[0]][empty_cell[1]] = 0

    return False


if __name__ == "__main__":
    board, regions = generate_board()
    solve(board, regions)
    print_board(board)
