package example

/* Define the different formats that are accepted" */
trait FormatType
case object money extends FormatType // localized fixed-point currency, "($3.50)"
case object space extends FormatType  // space-filled and signed, "                 +42"
case object dot extends FormatType // dot-filled and centered, ".........42........."
case object thousand extends FormatType  // space-filled and signed, " 5,000"