param (
	[string] $FileName = $( Read-Host "Enter path to fuzz:" ),
	[Int32]  $FuzzMax  = 1000
)

Write-Host "`nBuilding dev binary"

Start-Process -FilePath "cargo.exe" -Wait -NoNewWindow -ArgumentList "build"

Write-Host "`nBuilding release binary"

Start-Process -FilePath "cargo.exe" -Wait -NoNewWindow -ArgumentList "build", "--release"

Write-Host "`nFuzzing...`n"

$StartTime = $(get-date)

$options = @{
	PassThru = $true
	NoNewWindow = $true
	RedirectStandardError = "Errors.txt"
	ErrorAction = "Stop"
}

for ($counter = 1; $counter -le $FuzzMax ; $counter += 1) {
	Start-Process @options -FilePath "mscript.exe" -ArgumentList "run", "$FileName", "--quick" | Out-Null
	Write-Host -NoNewline "`r$counter"
	
	Start-Process @options -FilePath "cargo.exe" -ArgumentList "run", "--", "run", "$FileName", "--quick" | Out-Null
	Write-Host -NoNewline "`r$counter"



	if ($counter % 100 -eq 0) {
		$elapsedTime = $(get-date) - $StartTime

		$totalTime = "{0:HH:mm:ss}" -f ([datetime] $elapsedTime.Ticks)

		Write-Host "`n[elapsed $totalTime] Progress Check: $($counter / $FuzzMax * 100)% ($counter/$FuzzMax)"
	}
}

