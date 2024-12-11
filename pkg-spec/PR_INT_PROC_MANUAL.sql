CREATE OR REPLACE PACKAGE PR_INT_PROC_MANUAL AS

/*
REVISIONES:
  Version       Fecha        Autor          Incidencia      Cambio realizado
--------------------------------------------------------------------------------
    1.1       28/06/2023     COTECSA-MX        -            Paquete de Utilidades para Polizas.(Ajustes)
    1.0       28/06/2023     COTECSA-MX        -            Paquete de Utilidades para Polizas.
*/

--|| APOYO ||--
PROCEDURE   ANULAR_OBLIGACION ( pNumOblig NUMBER );
PROCEDURE   ANULAR_REL_ING ( pNumRelIng NUMBER );
PROCEDURE   BORRAR_MOD_COBERT  ( pIdePol NUMBER,pNumCert NUMBER,pNumOper NUMBER );

FUNCTION    APLICAR_COBRO_WEB ( pIdeFact NUMBER,pMonto NUMBER ) RETURN NUMBER;

--|| PRINCIPAL ||--
PROCEDURE   REVERTIR_RECAUDO_POLWEB ( pIdePol NUMBER,pNumCert NUMBER,pModCobert VARCHAR2 );
PROCEDURE   REVERTIR_OBLIG_DEVOLUCION ( pIdePol NUMBER,pNumCert NUMBER,pModCobert VARCHAR2 );
PROCEDURE   REVERTIR_COMPENSACION ( pIdePol NUMBER,pNumCert NUMBER );
PROCEDURE   REHABILITAR_POL_SIN_OPERPOL ( pIdePol NUMBER );

--|| INTERFAZ ||--
PROCEDURE   EJEC_PROC_REHAB ( pIdeProceso NUMBER,pIdJob NUMBER,pCodigo VARCHAR2,pIdCarga NUMBER,pFechaIni DATE,pFechaFin DATE );

PROCEDURE   INSERT_PROC_REHAB_POLIZA ( pRowREHAB PROC_REHAB_POLIZA%ROWTYPE );

END;
/
CREATE OR REPLACE PACKAGE BODY PR_INT_PROC_MANUAL AS
/

