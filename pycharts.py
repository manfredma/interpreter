from pyecharts import options as opts
from pyecharts.charts import Tree

data = \
    [
        {
            "children":
                [
                    {
                        "children":
                            [
                                {
                                    "children":
                                        [{'name': 'D'}, {'name': 'E'}],
                                    "name": "C"
                                },
                                {
                                    "children":
                                        [{'name': 'J'}, {'name': 'K'}],
                                    "name": "H"
                                },
                                {
                                    "children":
                                        [{'name': 'Y'}, {'name': 'Z'}],
                                    "name": "X"
                                },
                            ],
                        "name": "B"
                    }
                ],
            "name": "A"
        }
    ]

tree = (
    Tree()
    .add("", data, initial_tree_depth=-1, orient="TB")
    .set_global_opts(title_opts=opts.TitleOpts(title="Tree-基本示例"))
)

tree.render('out/Tree.html')
