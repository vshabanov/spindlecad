Пробная реализация Imperative Reactive Programming на Haskell.

> import System.IO.Unsafe
> import Data.IORef
> import Prelude hiding (const)

Для начала чисто функциональная реализация (без пересчета).
Надо ее вынести в отдельный модуль что-ли (с абстрактными типами данных).
Или типы превратить в data, а то они просто подставляются.

> type Var a = a -- IORef a

> type Value s a = s -> Var a

> value :: (s -> Var a) -> Value s a
> value f = f

> instantiate :: Value s a -> s -> a
> instantiate v s = v s

> get :: Var a -> a
> get a = a

> lift :: (a -> b) -> Value s a -> Value s b
> lift f v = value $ \ s ->
>            f (get $ instantiate v s)

> const :: a -> Value s a
> const a = value $ \ _ -> a

> -- x :: Value s Integer все равно говорит x :: s -> Var Integer, т.е.
> -- type сразу подставляется
> x = const 1
> sh = lift show
> top = sh x

> main =
>     print $ get $ instantiate top ()
