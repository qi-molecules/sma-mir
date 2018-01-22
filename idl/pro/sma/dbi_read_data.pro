function dbi_read_data

common global
common data_set
common wlm

    result=dbi_head_read()
    result=dbi_chan_read()

    return,1

end
