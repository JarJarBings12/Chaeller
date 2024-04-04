module program
open pda
open vis

runPda (parseExpression "11+11++") displayReport
        
