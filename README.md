# Kompilator języka Instant

Napisany w Haskell'u. Kompiluje do JVM lub LLVM. W pierwszym przypadku generuje plik `*.j`, który z pomocą jasmina jest kompilowany do pliku `*.class`. W drugim przypadku generuje plik `*.ll`, który poźniej jest kompilowany do pliku `*.bc`.

## Uruchomienie
- polecenie `make` buduje dwa pliki wykonwywalne `insc_jvm` i `insc_llvm`
- polecenie `./insc_jvm test.ins` wygeneruje pliki `test.j` i `test.class`, aby uruchomić program użyj polecenia `java test`
- polecenie `./insc_llvm test.in` wygeneruje plik `test.bc`, aby urochomić program użyj polecenia `lli test.bc`

## Biblioteki
W katalogu `lib` znajdują się pliki:
- `jasmin.jar` - służy do kompliacji pliku jasmin'owego
- `runtime.bc` - skompilowane funkcje pomocniczne dla LLVM, między innymi `printInt`

