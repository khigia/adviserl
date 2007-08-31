{application, adviserl, [
    {description, "Recommender system."},
    {vsn, "0.2"},
    {modules, [
        adv_adviserl_sup,
        adv_api,
        adv_array_ext,
        adv_config,
        adv_data_ets,
        adv_data_mnesia,
        adv_items,
        adv_mat_dm,
        adv_mat_sm,
        adv_mnesia,
        adv_predictions,
        adv_ratings,
        adv_ratings_dod,
        adv_ratings_mnesia,
        adv_slone_mnesia,
        adv_slone_smdod,
        adv_sources,
        adv_types,
        adv_util,
        adviserl
    ]},
    {registered, [
        adv_sources,
        adv_items,
        adv_ratings,
        adv_predictions
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
            {dir,    "./data/mnesia-adviserl/"},
            {backup, [
                {file, "./data/backup-mnesia-adviserl.backup"},
                {tables, [adv_sources, adv_items, adv_ratings, adv_predictions]}
            ]}
        ]},
        {inets, [
            start
        ]},
        {sources,     {adv_data_mnesia,    [adv_sources]}},
        {items,       {adv_data_mnesia,    [adv_items]}},
        {ratings,     {adv_ratings_mnesia, [adv_ratings]}},
        {predictions, {adv_slone_mnesia,   [adv_predictions]}}
    ]}
]}.
