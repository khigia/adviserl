{application, adviserl, [
    {description, "Recommender system."},
    {vsn, "0.1"},
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
        {recommender, {adv_slone, []}}
    ]}
]}.
