["date","symbol","close"],
(
  .[]
  | .date as $date
  | .rates[]
  | [$date,.ccy1 + .ccy2,.rate]
)
  | @csv
