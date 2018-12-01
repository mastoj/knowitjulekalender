open System

let start = new DateTime(1970,1,1,0,0,0, DateTimeKind.Utc)
let shiftTime = new DateTime(1814,5,17,13,37,14, DateTimeKind.Utc)
let span = shiftTime - start
let endTime = new DateTime(2015,9,17,17,15,0, DateTimeKind.Utc)
let endShiftTime = endTime + span
let seconds = (endTime - shiftTime).TotalSeconds