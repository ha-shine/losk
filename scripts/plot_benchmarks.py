import plotly.graph_objects as go


def parse_file(name):
    fname = "./bench/{}.csv".format(name)
    file = open(fname)
    data = {
        "name": name,
        "data": {},
    }
    for line in file.readlines():
        parts = line.split(",")

        # Skip the header row
        if parts[0].startswith("command"):
            continue

        prog = ""
        if parts[0].startswith("./clox-rs"):
            prog = "rust"
        elif parts[0].startswith("./clox"):
            prog = "c"
        elif parts[0].startswith("python"):
            prog = "python3.9.2"

        data["data"][prog] = float(parts[1])
    return data


def main():
    files = [
        "binary_trees",
        "equality",
        "fib",
        "instantiation",
        "invocation",
        "method_call",
        "properties",
        "trees",
        "zoo"
    ]

    data = {
        "c": [],
        "rust": [],
        "python3.9.2": [],
    }
    colors = {"c": "#555555", "rust": "#DEA584", "python3.9.2": "#3572A5"}

    langs = ["c", "rust", "python3.9.2"]

    for file in files:
        parsed = parse_file(file)
        for lang in langs:
            data[lang].append(parsed["data"][lang])

    fig = go.Figure(
        data=[
            go.Bar(
                name=lang,
                x=files,
                y=data[lang],
                marker_color=colors[lang],
            ) for lang in ["c", "rust", "python3.9.2"]
        ],
    )
    fig.update_layout(
        xaxis_tickfont_size=14,
        yaxis={
            "title": "Execution Time (seconds)",
        },
        bargroupgap=0.1,
        barmode="group"
    )
    fig.write_image("./benchmark.png", width=1400, height=800)


if __name__ == "__main__":
    main()
