import usuarios.*
import productos.*


object PdePLibre {
    const usuarios = #{}
    const productos = []

    method penalizarUsuariosMorosos(){
        usuarios.filter({usuario => usuario.esMoroso()})
                .forEach({usuario => usuario.perderPuntos(1000)})
    }

    method eliminarCuponesUsados(){
        usuarios.forEach({usuario => usuario.eliminarCuponesUsados()})
    }

    method nombresDeOfertaDeProductos(){
        productos.map({producto => producto.nombreDeOferta()})
    }

    method actualizarNivelesDeUsuarios(){
        usuarios.forEach({usuario => usuario.actualizarNivel()})
    }
}