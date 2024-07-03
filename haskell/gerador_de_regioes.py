def parse_regions(grid):
    regions = {}
    for i, row in enumerate(grid):
        for j, cell in enumerate(row):
            if cell not in regions:
                regions[cell] = []
            regions[cell].append((i, j))
    return regions

def format_regions(regions):
    region_strings = []
    for region_id, cells in sorted(regions.items()):
        region_strings.append(f'    ({region_id}, {cells})')
    return ',\n'.join(region_strings)

def write_result_file(filename, content):
    with open(filename, 'w') as f:
        f.write(content)

# Tabela de entrada
grid = [
    [1, 2, 2, 2, 4, 5],
    [1, 3, 2, 4, 4, 4],
    [1, 1, 6, 4, 7, 7],
    [8, 9, 6, 10, 10, 7],
    [8, 9, 9, 11, 11, 7],
    [9, 9, 9, 11, 11, 11]
]

# Processa as regiões
regions = parse_regions(grid)

# Formata as regiões para saída
regions_content = 'regions_6x6 = \n[\n' + format_regions(regions) + '\n]'

# Escreve no arquivo result.txt
write_result_file('result.txt', regions_content)