function ymdut_to_unixtime, year, month, day, uthours
  unixtime = 86400*(julday(month, day, year) - julday(1,1,1970)) + uthours*3600
  return, unixtime
end
