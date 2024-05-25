import Data.Time.Clock
import Data.Time.Format
import System.IO
import Control.Exception (try, IOException)
import Control.DeepSeq (deepseq)
import Data.List (find)
import Data.Maybe (isNothing)

-- Definición del tipo de datos para representar la información de un vehículo
data Vehiculo = Vehiculo {
    placa :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime -- Usamos Maybe para representar que el vehículo aún está en el parqueadero o ya salió
} deriving (Show, Read)

-- Función para registrar la entrada de un vehículo al parqueadero
registrarEntrada :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarEntrada placaVehiculo tiempo parqueadero =
    if estaVehiculoEnParqueadero placaVehiculo parqueadero
    then parqueadero
    else Vehiculo placaVehiculo tiempo Nothing : parqueadero

-- Función para registrar la salida de un vehículo del parqueadero
registrarSalida :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarSalida placaVehiculo tiempo parqueadero =
    map (\v -> if placaVehiculo == placa v then v { salida = Just tiempo } else v) parqueadero

-- Función para buscar un vehículo por su placa en el parqueadero
buscarVehiculo :: String -> [Vehiculo] -> Maybe Vehiculo
buscarVehiculo placaVehiculo parqueadero =
    find (\v -> placaVehiculo == placa v && isNothing (salida v)) parqueadero

-- Función para comprobar si un vehículo está actualmente en el parqueadero
estaVehiculoEnParqueadero :: String -> [Vehiculo] -> Bool
estaVehiculoEnParqueadero placaVehiculo parqueadero =
    case buscarVehiculo placaVehiculo parqueadero of
        Just _  -> True
        Nothing -> False

-- Función para calcular el tiempo que un vehículo permaneció en el parqueadero
tiempoEnParqueadero :: Vehiculo -> UTCTime -> NominalDiffTime
tiempoEnParqueadero vehiculo tiempoActual =
    diffUTCTime tiempoActual (entrada vehiculo)

-- Función para convertir un vehículo a un formato de texto fácil de leer
formatoVehiculoLegible :: Vehiculo -> String
formatoVehiculoLegible (Vehiculo placa entrada (Just salida)) =
    placa ++ "," ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" entrada ++ "," ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" salida
formatoVehiculoLegible (Vehiculo placa entrada Nothing) =
    placa ++ "," ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" entrada ++ ","

-- Función para guardar la información de los vehículos en un archivo de texto
guardarParqueadero :: [Vehiculo] -> IO ()
guardarParqueadero parqueadero = do
    withFile "parqueadero.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map formatoVehiculoLegible parqueadero))
    putStrLn "Parqueadero guardado en el archivo parqueadero.txt."

-- Función para cargar la lista de vehículos desde un archivo de texto
leerParqueaderoDesdeArchivo :: IO [Vehiculo]
leerParqueaderoDesdeArchivo = do
    resultado <- try (readFile "parqueadero.txt") :: IO (Either IOException String)
    case resultado of
        Left ex -> do
            putStrLn $ "Advertencia: No se pudo leer el archivo parqueadero.txt: " ++ show ex
            return []
        Right contenido -> do
            contenido `deepseq` return ()  -- Forzar la evaluación completa del contenido
            let parqueadero = map convertirTextoAVehiculo (lines contenido)
            return parqueadero

-- Función para transformar una línea de texto en un registro de vehículo
convertirTextoAVehiculo :: String -> Vehiculo
convertirTextoAVehiculo linea =
    let [placa, entrada, salida] = separar ',' linea
    in Vehiculo placa (leerTiempo entrada) (if salida == "" then Nothing else Just (leerTiempo salida))

-- Función auxiliar para leer el tiempo desde una cadena de texto
leerTiempo :: String -> UTCTime
leerTiempo = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S"

-- Función auxiliar para dividir una cadena de texto por un singular
separar :: Char -> String -> [String]
separar _ "" = [""]
separar delimitador str =
    foldr (\c acc -> if c == delimitador then "":acc else (c:head acc):tail acc) [""] str

-- Función para eliminar un vehículo por su placa del parqueadero
eliminarVehiculo :: String -> [Vehiculo] -> [Vehiculo]
eliminarVehiculo placaVehiculo = filter (\v -> placa v /= placaVehiculo)

-- Crear formato de las listas
formatoVehiculo :: Vehiculo -> String
formatoVehiculo vehiculo =
    "Vehiculo {PLACA: " ++ formatoVehiculoLegible vehiculo ++ "}"

main :: IO ()
main = do
    parqueaderoInicial <- leerParqueaderoDesdeArchivo
    putStrLn "¡Bienvenido al Sistema de Gestión de Parqueadero!"
    cicloPrincipal parqueaderoInicial

-- Función principal
cicloPrincipal :: [Vehiculo] -> IO ()
cicloPrincipal parqueadero = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de vehículo"
    putStrLn "2. Registrar salida de vehículo"
    putStrLn "3. Buscar vehículo por placa"
    putStrLn "4. Ver vehiculos en el parqueadero"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la placa del vehículo:"
            placaVehiculo <- getLine
            if estaVehiculoEnParqueadero placaVehiculo parqueadero
            then do
                putStrLn $ "El vehículo con placa " ++ placaVehiculo ++ " ya se encuentra en el parqueadero."
                cicloPrincipal parqueadero
            else do
                tiempoActual <- getCurrentTime
                let parqueaderoActualizado = registrarEntrada placaVehiculo tiempoActual parqueadero
                putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " ingresado al parqueadero."
                guardarParqueadero parqueaderoActualizado
                cicloPrincipal parqueaderoActualizado

        "2" -> do
            putStrLn "Ingrese la placa del vehículo a salir:"
            placaVehiculo <- getLine
            case buscarVehiculo placaVehiculo parqueadero of
                Just vehiculo -> do
                    tiempoActual <- getCurrentTime
                    let parqueaderoActualizado = eliminarVehiculo placaVehiculo (registrarSalida placaVehiculo tiempoActual parqueadero)
                    putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " ha salido del parqueadero."
                    guardarParqueadero parqueaderoActualizado
                    cicloPrincipal parqueaderoActualizado
                Nothing -> do
                    putStrLn "Vehículo no encontrado en el parqueadero o ya ha salido."
                    cicloPrincipal parqueadero

        "3" -> do
            putStrLn "Ingrese la placa del vehículo a buscar:"
            placaVehiculo <- getLine
            tiempoActual <- getCurrentTime
            case buscarVehiculo placaVehiculo parqueadero of
                Just vehiculo -> do
                    let tiempoTotal = tiempoEnParqueadero vehiculo tiempoActual
                    putStrLn $ "El vehículo con placa " ++ placaVehiculo ++ " se encuentra en el parqueadero."
                    putStrLn $ "Tiempo en parqueadero: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Vehículo no encontrado en el parqueadero."
            cicloPrincipal parqueadero

        "4" -> do
            contenido <- readFile "parqueadero.txt"
            let lineas  = lines contenido
                vehiculos = map convertirTextoAVehiculo lineas
                vehiculosFormateados = map formatoVehiculo vehiculos
            mapM_ putStrLn vehiculosFormateados
            cicloPrincipal parqueadero

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal parqueadero

