# Запуск

Собирать проект при помощи [`stack`](https://docs.haskellstack.org/en/stable/README/). 

* `stack build` -- сборка 

* `stack run main -- filename` -- парсинг файла filename в filename.out

* `stack test` -- запуск юнит-тестов

* `stack ghci` -- запуск `ghci` со всеми зависимостями из проекта, подгружает исполнимый Main 

# Тесты 

Тесты находятся в двух файлах в папке [test/Test/](test/Test).