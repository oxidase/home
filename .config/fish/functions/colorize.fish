function colorize
    while read -l input
        switch (string lower $input)
            case "[error]*"
                set_color red
                echo $input
            case "[warn]*"
                set_color F58
                echo $input
            case "[info]*"
                set_color 5AE
                echo $input
            case "*"
                set_color normal
                echo $input
        end
    end
    set_color normal
end
