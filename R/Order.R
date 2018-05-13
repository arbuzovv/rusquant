Order <- function(Symbol = "",
                 Action = "BUY",
                 Quantity = 10,
                 OrderType = "LMT",
                 Price = 0)
{
structure(list(Symbol=Symbol,Action=Action,Quantity=Quantity,OrderType=OrderType,Price=Price),
                        class='Order')
}