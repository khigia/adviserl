{application, adviserl, [
    {description, "Recommender system."},
    {vsn, "0.2"},
    {modules, [
        adv_adviserl_sup,
        adv_items,
        adv_ratings,
        adv_mat_dm,
        adv_mat_sm,
        adv_array_ext,
        adv_util,
        adv_types,
        adviserl
    ]},
    {registered, [
    ]},
    {applications, [
        kernel,
        stdlib,
        sasl
    ]},
    {mod, {adviserl, [
    ]}},
    {env, [
        {mnesia, [
            {dir, "./data/mnesia-adviserl/"}
        ]},
        {sources,     {adv_data_mnesia,    [advtest_sources]}},
        {items,       {adv_data_mnesia,    [advtest_items]}},
        {ratings,     {adv_ratings_mnesia, [advtest_ratings]}},
        {predictions, {adv_slone_mnesia,   [advtest_predictions]}}
    ]}
]}.
