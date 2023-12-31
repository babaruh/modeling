# Рекурсивні завдання на F#

Цей проект містить п'ять рекурсивних функцій, написаних на мові програмування F#. Ці функції розв'язують такі задачі:

- Друк строки у зворотному порядку
- Зміна місцями кожної пари сусідніх вузлів у зв'язаному списку
- Обчислення n-го числа Фібоначчі
- Підрахунок кількості способів піднятися по сходах за один або два кроки
- Піднесення числа до ступеня

## Встановлення

Для запуску цього проекту вам потрібно мати встановлений комп'ютер F# compiler та dotnet core. Ви також можете скористатися онлайн-компілятором F#, наприклад repl.it.

## Використання

Ви можете запустити цей проект за допомогою команди `dotnet run` у терміналі або скопіювати код у онлайн-компілятор. Кожна функція приймає певний тип аргументу та повертає певний тип результату. Ось приклади виклику кожної функції:

```fsharp
// Друк строки у зворотному порядку
let str = "tiger"
let reversed = reverseString str // "regit"

// Зміна місцями кожної пари сусідніх вузлів у зв'язаному списку
type Node<'T> = 
    | Empty
    | Cons of 'T * Node<'T>

let list = Cons(1, Cons(2, Cons(3, Cons(4, Empty)))) // [1; 2; 3; 4]
let swapped = swapPairs list // [2; 1; 4; 3]

// Обчислення n-го числа Фібоначчі
let n = 4
let fib_n = fib n // 3

// Підрахунок кількості способів піднятися по сходах за один або два кроки
let steps = 3
let ways = climbStairs steps // 3

// Піднесення числа до ступеня
let x = 2.0
let n = -2.0
let x_pow_n = pow x n // 0.25
```

## Ліцензія

Цей проект ліцензований за умовами GNU license.
