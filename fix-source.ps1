$crateMappings = @{
    'rosetta_core' = 'rosetta_stone_core'
    'rosetta_ir' = 'rosetta_stone_ir'
    'rosetta_codegen' = 'rosetta_stone_codegen'
    'rosetta_c' = 'rosetta_stone_c'
    'rosetta_wasm' = 'rosetta_stone_wasm'
    'rosetta_fortran' = 'rosetta_stone_fortran'
    'rosetta_cobol' = 'rosetta_stone_cobol'
    'rosetta_pli' = 'rosetta_stone_pli'
    'rosetta_lisp' = 'rosetta_stone_lisp'
    'rosetta_quickbasic' = 'rosetta_stone_quickbasic'
    'rosetta_ml' = 'rosetta_stone_ml'
    'rosetta_mumps' = 'rosetta_stone_mumps'
    'rosetta_rpg' = 'rosetta_stone_rpg'
    'rosetta_rexx' = 'rosetta_stone_rexx'
    'rosetta_pascal' = 'rosetta_stone_pascal'
    'rosetta_algol' = 'rosetta_stone_algol'
    'rosetta_modula2' = 'rosetta_stone_modula2'
    'rosetta_ada' = 'rosetta_stone_ada'
    'rosetta_apl' = 'rosetta_stone_apl'
    'rosetta_snobol' = 'rosetta_stone_snobol'
    'rosetta_simula' = 'rosetta_stone_simula'
    'rosetta_smalltalk' = 'rosetta_stone_smalltalk'
    'rosetta_forth' = 'rosetta_stone_forth'
    'rosetta_planner' = 'rosetta_stone_planner'
    'rosetta_ops5' = 'rosetta_stone_ops5'
    'rosetta_krl' = 'rosetta_stone_krl'
    'rosetta_prolog' = 'rosetta_stone_prolog'
    'rosetta_clips' = 'rosetta_stone_clips'
    'rosetta_cli' = 'rosetta_stone_cli'
    'rosetta_validator' = 'rosetta_stone_validator'
    'rosetta_gui' = 'rosetta_stone_gui'
    'rosetta_docs' = 'rosetta_stone_docs'
}

$rsFiles = Get-ChildItem -Path "C:\Users\pakom\Rosetta\crates" -Recurse -Filter "*.rs"
$count = 0

foreach ($file in $rsFiles) {
    $content = Get-Content $file.FullName -Raw
    $modified = $false

    foreach ($old in $crateMappings.Keys) {
        $new = $crateMappings[$old]
        if ($content -match $old) {
            $content = $content -replace $old, $new
            $modified = $true
        }
    }

    if ($modified) {
        Set-Content $file.FullName $content -NoNewline
        $count++
        Write-Host "Fixed: $($file.FullName)"
    }
}

Write-Host "Total files updated: $count"
