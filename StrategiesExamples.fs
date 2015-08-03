module MachineLearning.StrategiesExamples

open System
open MachineLearning.Options

let strangle strike1 strike2 stock = 
    {
        Rate = 0.01
        Name = "Long Strangle"
        Legs = [ 
                OptionLeg { 
                    Direction = 1.0
                    Strike = strike1
                    Expiry = DateTime.Now.AddDays(10.0)
                    Kind = Call
                };
                OptionLeg { 
                    Direction = 1.0
                    Strike = strike2
                    Expiry = DateTime.Now.AddDays(10.0)
                    Kind = Put
                }
        ]
        Stock = stock
    }

let straddle strike stock = 
    {
        Rate = 0.01
        Name = "Straddle"
        Legs = [ 
                OptionLeg { 
                    Direction = 1.0
                    Strike = strike
                    Expiry = DateTime.Now.AddDays(20.0)
                    Kind = Call
                };
                OptionLeg { 
                    Direction = 1.0
                    Strike = strike
                    Expiry = DateTime.Now.AddDays(20.0)
                    Kind = Put
                }
        ]
        Stock = stock
    }
let butterfly strike1 strike2 stock = {
        Rate = 0.01
        Name = "Long Strangle"
        Legs = [ 
                OptionLeg { 
                    Direction = 1.0
                    Strike = strike1
                    Expiry = DateTime.Now.AddDays(10.0)
                    Kind = Call
                };
                OptionLeg { 
                    Direction = -1.0
                    Strike = strike2
                    Expiry = DateTime.Now.AddDays(10.0)
                    Kind = Put
                };
                OptionLeg { 
                    Direction = 2.0
                    Strike = (strike1 + strike2) / 2.0
                    Expiry = DateTime.Now.AddDays(10.0)
                    Kind = Put
                }
        ]
        Stock = stock
    }

let riskReversal strike stock = 
    {
        Rate = 0.01
        Name = "Risk Reversal"
        Legs = [ 
                OptionLeg { 
                    Direction = 1.0
                    Strike = strike
                    Expiry = DateTime.Now.AddDays(10.0)
                    Kind = Call
                };
                OptionLeg { 
                    Direction = -1.0
                    Strike = strike
                    Expiry = DateTime.Now.AddDays(10.0)
                    Kind = Put
                }
        ]
        Stock = stock
    }

let collar strike1 strike2 stock = 
    {
        Rate = 0.01
        Name = "Collar"
        Legs = [ 
                OptionLeg { 
                    Direction = 1.0
                    Strike = strike1
                    Expiry = DateTime.Now.AddDays(10.0)
                    Kind = Call
                };
                OptionLeg { 
                    Direction = -1.0
                    Strike = strike2
                    Expiry = DateTime.Now.AddDays(10.0)
                    Kind = Put
                };
                CashLeg { 
                    Direction = 1.0
                    Strike = strike1
                }
        ]
        Stock = stock
    }

let cashPayOff strike ref = ref - strike

let condor stock = 
    {
        Rate = 0.01
        Name = "Condor"
        Legs = [ 
                OptionLeg { 
                    Direction = -1.0
                    Strike = 14.0
                    Expiry = DateTime.Now.AddDays(10.0)
                    Kind = Call
                };
                OptionLeg { 
                    Direction = -1.0
                    Strike = 12.0
                    Expiry = DateTime.Now.AddDays(10.0)
                    Kind = Call
                };
                OptionLeg { 
                    Direction = -1.0
                    Strike = 20.0
                    Expiry = DateTime.Now.AddDays(10.0)
                    Kind = Call
                };
                OptionLeg { 
                    Direction = -1.0
                    Strike = 22.0
                    Expiry = DateTime.Now.AddDays(10.0)
                    Kind = Call
                }
        ]
        Stock = stock
    }

let boxOption strike1 strike2 stock = 
    {
        Rate = 0.01
        Name = "Collar"
        Legs = [ 
                OptionLeg { 
                    Direction = 1.0
                    Strike = strike1
                    Expiry = DateTime.Now.AddDays(10.0)
                    Kind = Call
                };
                OptionLeg { 
                    Direction = -1.0
                    Strike = strike2
                    Expiry = DateTime.Now.AddDays(10.0)
                    Kind = Call
                };
                OptionLeg { 
                    Direction = 1.0
                    Strike = strike2
                    Expiry = DateTime.Now.AddDays(10.0)
                    Kind = Call
                };
                OptionLeg { 
                    Direction = -1.0
                    Strike = strike1
                    Expiry = DateTime.Now.AddDays(10.0)
                    Kind = Call
                }
        ]
        Stock = stock
    }
