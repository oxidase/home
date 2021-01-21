function colorize
    while read -l input
        switch $input
            case "EE:*"
                set_color F66
                echo $input
            case "WW:*"
                set_color AD7FA8
                echo $input
            case "II:*"
                set_color 5AE
                echo $input
            case "*"
                echo $input
        end
        set_color normal
    end
end
