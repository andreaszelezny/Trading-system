#r @"..\packages\FSharp.Data.2.0.15\lib\net40\FSharp.Data.dll"
#load "../packages/MathNet.Numerics.FSharp.3.2.3/MathNet.Numerics.fsx"
open MathNet.Numerics.Statistics
open MathNet.Numerics.LinearAlgebra
open FSharp.Data
open System.IO
open System.Collections.Generic

let readSPSymbols filename =
    File.ReadAllLines(filename)
    |> List.ofArray

type Yahoo = CsvProvider< @"C:\Yahoo\A.csv",InferRows = 5>

let getYahooData symbol dateRange =
    let data = Yahoo.Load(@"C:\Yahoo\" + symbol + ".csv")
    match dateRange with
    | Some(loDate, hiDate) -> 
        [ for r in data.Rows do 
            if r.Date >= loDate && r.Date <= hiDate then yield r.Date, float r.``Adj Close`` ] |> List.rev
    | _ -> [ for r in data.Rows -> r.Date, float r.``Adj Close`` ] |> List.rev

let getBollinger symbol startDate endDate lookbackdays =
    let ydata = getYahooData symbol (Some(startDate,endDate))
    let d = Seq.windowed lookbackdays ydata
    let rolmeansd = Seq.map (fun x -> fst (Array.get x (lookbackdays-1)), Seq.map snd x |> Statistics.MeanStandardDeviation) d
    let rolmean = Seq.map (fun (t,v) -> (t, fst v)) rolmeansd
    let rolsd = Seq.map (fun (t,v) -> (t, snd v)) rolmeansd
    let pricesMap = Map.ofList ydata
    let indicatorvalues = Seq.map2 (fun t1 t2 -> fst t1, (pricesMap.[fst t1] - snd t1) / snd t2) rolmean rolsd
    indicatorvalues

let bolliSPY = getBollinger "SPY" startDate endDate 20 |> Map.ofSeq

type Ordertype = 
    | Sell
    | Buy

type Order = { Date: System.DateTime; Symbol: string; Type: Ordertype; Count: int }

let calcEvents symbol startDate endDate (orderList: List<Order>) buyamount holdingdays =
    let blist = getBollinger symbol startDate endDate 20 |> List.ofSeq  // 20 = lookback days
    blist
    |> Seq.pairwise
    |> Seq.iteri (fun i (p1,p2) -> 
        if snd p1 >= -2.0 && snd p2 <= -2.0 && bolliSPY.[fst p2] >= 1.0 then 
            orderList.Add { Date = fst p2; Symbol = symbol; Type = Buy; Count = buyamount }
            let saleindex = if i + holdingdays + 1 > blist.Length-1 then blist.Length-1 else i + holdingdays + 1
            orderList.Add { Date = fst blist.[saleindex]; Symbol = symbol; Type = Sell; Count = buyamount }
            )

let writevalues (datelist: System.DateTime list) (pflist: float array) filename =
    use sw = File.CreateText filename
    for i = 0 to datelist.Length-1 do
        let datum = datelist.[i]
        sw.WriteLine("{0}, {1}, {2}, {3}", datum.Year, datum.Month, datum.Day, pflist.[i])

let marketsim cash (orders: Order list) valuesfilename =
    let orders = Seq.toList orderList
    let startDate = (List.minBy (fun o -> o.Date) orders).Date //new System.DateTime(2008,1,1)
    let endDate = (List.maxBy (fun o -> o.Date) orders).Date //new System.DateTime(2009,12,31) //orders.[orders.Length-1].Date
    let symbList = List.fold (fun acc o -> Set.add o.Symbol acc) Set.empty orders |> Set.toList
    let dateList = getYahooData "SPY" (Some(startDate, endDate)) |> List.map fst 
    let priceMatrix = Seq.init (List.length symbList) (fun i -> 
        getYahooData symbList.[i] (Some(startDate, endDate))
        |> List.map snd) |> DenseMatrix.ofColumnSeq
    let tradeMatrix = DenseMatrix.zero<float> priceMatrix.RowCount symbList.Length
    let cashMatrix = DenseMatrix.zero<float> priceMatrix.RowCount 1
    cashMatrix.[0,0] <- cash
    for o in orders do
        let r = List.findIndex ((=) o.Date) dateList
        let s = List.findIndex ((=) o.Symbol) symbList
        tradeMatrix.[r,s] <- (if o.Type=Buy then tradeMatrix.[r,s] + float o.Count else tradeMatrix.[r,s] - float o.Count)
        let cost = priceMatrix.[r,s] * (float o.Count)
        cashMatrix.[r,0] <- (if o.Type=Buy then cashMatrix.[r,0]-cost else cashMatrix.[r,0]+cost)
    let priceMatrix2 = priceMatrix.Append (DenseMatrix.init priceMatrix.RowCount 1 (fun _ _ -> 1.0))
    let tradeMatrix2 = tradeMatrix.Append cashMatrix
    tradeMatrix2.MapIndexedInplace (fun r c v -> if r = 0 then v else v + tradeMatrix2.[r-1,c])
    let holdingMatrix = tradeMatrix2
    let result = (holdingMatrix.PointwiseMultiply priceMatrix2).RowSums()
    writevalues dateList (result.ToArray()) valuesfilename

type Valuesfile = CsvProvider< @"C:\values.csv",InferRows = 5,HasHeaders = false>

let getValues (filename: string) =
    let data = Valuesfile.Load(filename)
    [ for r in data.Rows do 
        yield (new System.DateTime(r.Column1, r.Column2, r.Column3), float r.Column4) ]

let analyze valuesfilename symbol =
    let values = getValues valuesfilename
    let startDate = fst values.Head
    let endDate = fst values.[values.Length-1]
    let benchmark = getYahooData symbol (Some(startDate, endDate))
    let dailyret1 = List.ofSeq (Seq.append (Seq.singleton 0.) (values |> List.map (fun (d,v) -> v) |> Seq.pairwise |> Seq.map (fun (f,s) -> s / f - 1.)))
    let dailyret2 = List.ofSeq (Seq.append (Seq.singleton 0.) (benchmark |> List.map (fun (d,v) -> v) |> Seq.pairwise |> Seq.map (fun (f,s) -> s / f - 1.)))
    let avgret1 = List.average dailyret1
    let sdret1 = Statistics.PopulationStandardDeviation dailyret1
    let sharpeRatio1 = (sqrt 252.) * avgret1 / sdret1
    let avgret2 = List.average dailyret2
    let sdret2 = Statistics.PopulationStandardDeviation dailyret2
    let sharpeRatio2 = (sqrt 252.) * avgret2 / sdret2
    printfn "Details of performance of portfolio:"
    printfn "Data range: %A to %A" startDate endDate
    printfn "Sharpe ratio of fund: %f" sharpeRatio1
    printfn "Sharpe ratio of %s: %f" symbol sharpeRatio2
    printfn "Total return of fund: %f" ((snd values.[values.Length-1] / snd values.Head))
    printfn "Total return of %s: %f" symbol (snd benchmark.[benchmark.Length-1] / snd benchmark.Head)
    printfn "Standard deviation of fund: %f" sdret1
    printfn "Standard deviation of %s: %f" symbol sdret2
    printfn "Average daily return of fund: %f" avgret1
    printfn "Average daily return of %s: %f" symbol avgret2
    let basevalue = snd benchmark.Head
    let benchmark2 = List.map (fun (d,v) -> (d,v / basevalue * 1.0e6)) benchmark
    Chart.Combine( [
        Chart.Line(values, Name="Portfolio")
        Chart.Line(benchmark2, Name=symbol) ]).WithLegend()

// Main
let symlist = readSPSymbols @"C:\Yahoo\Lists\sp5002012.txt"
let startDate = new System.DateTime(2008,1,1)
let endDate = new System.DateTime(2009,12,1)
let orderList = new List<Order>()
List.iter (fun sym -> calcEvents sym startDate endDate orderList 100 5) symlist
saveOrders @"C:\orders.csv" orderList
marketsim 100000.0 (Seq.toList orderList) @"C:\values.csv"
