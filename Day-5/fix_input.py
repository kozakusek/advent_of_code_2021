with open("original_input.txt") as inp:
    with open("input.txt", "w") as out:
        out.writelines([line.replace(",", " ").replace("->", "") for line in inp.readlines()])
