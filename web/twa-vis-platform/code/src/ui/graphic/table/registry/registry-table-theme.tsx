import CssBaseline from '@mui/material/CssBaseline';
import { createTheme, ThemeProvider } from '@mui/material/styles';
import useMediaQuery from '@mui/material/useMediaQuery';
import { ReactNode, useMemo } from 'react';

const lightTheme = createTheme({
    palette: {
        mode: 'light',
        background: {
            default: '#e5e5e5',
            paper: '#efefef',
        },
        text: {
            primary: '#30363d',
            secondary: '#7f7f80',
        },
        primary: {
            main: '#146a7d',
        },
        secondary: {
            main: '#20aac9',
        },
        divider: '#b6b6bf',
    },
    typography: {
        fontFamily: '"Helvetica Now", sans-serif',
        fontSize: 12,
        h1: {
            fontSize: '1.8rem',
        },
        h2: {
            fontSize: '1.3rem',
        },
        body1: {
            fontSize: '0.95rem',
        },
        body2: {
            fontSize: '0.8rem',
        },
        caption: {
            fontSize: '0.7rem',
        },
    },
    components: {
        MuiContainer: {
            styleOverrides: {
                root: {
                    display: 'flex',
                    alignSelf: 'center',
                    flexDirection: 'column',
                    width: '90%',
                    height: '90vh',
                    margin: '1rem',
                },
            },
        },
        MuiPaper: {
            styleOverrides: {
                root: {
                    width: '90vw',
                    padding: '1rem 2rem',
                    margin: '1rem',
                    backgroundColor: 'var(--background-primary)',
                    border: '1px solid var(--border-primary) !important',
                    borderRadius: '20px',
                    flexDirection: 'column',
                },
            },
        },
        MuiTypography: {
            styleOverrides: {
                h2: {
                    margin: '0.5rem 0',
                    color: 'var(--text-color-primary)',
                    fontSize: 'var(--font-size-h2)',
                },
            },
        },
        MuiTableContainer: {
            styleOverrides: {
                root: {
                    height: '60vh',
                    width: '100%',
                    overflowY: 'auto',
                    overflowX: 'auto',
                },
            },
        },
        MuiTable: {
            styleOverrides: {
                root: {
                    width: '100%',
                    overflowX: 'hidden',
                    borderSpacing: 0,
                    borderCollapse: 'separate',
                },
            },
        },
        MuiTableHead: {
            styleOverrides: {
                root: {
                    position: 'sticky',
                    top: 0,
                    backgroundColor: 'var(--background-inverse-tertiary)',
                    textAlign: 'left',
                    textWrap: 'nowrap',
                },
            },
        },
        MuiTableCell: {
            styleOverrides: {
                head: {
                    padding: '0.5rem 1rem 0.5rem 0.4rem',
                    fontSize: 'var(--font-size-secondary-text)',
                    color: 'var(--text-color-primary)',
                    borderStyle: 'solid',
                    borderColor: 'var(--border-primary)',
                    borderWidth: '0.1rem 0',
                },
                body: {
                    whiteSpace: 'nowrap',
                    padding: '0.5rem 0.4rem',
                    fontSize: 'var(--font-size-tertiary-text)',
                    color: 'var(--text-color-secondary)',
                    borderStyle: 'solid',
                    borderColor: 'var(--border-secondary)',
                    borderWidth: '0 0 0.025rem',
                },
            },
        },
        MuiIconButton: {
            styleOverrides: {
                root: {
                    color: 'var(--text-color-links) !important',
                    '&:hover': {
                        color: 'var(--text-color-links-hover) !important',
                    },
                },
            },
        },
    },
});

const darkTheme = createTheme({
    palette: {
        mode: 'dark',
        background: {
            default: '#30363d',
            paper: '#30363d',
        },
        text: {
            primary: '#dde9ff',
            secondary: 'deepskyblue',
        },
        primary: {
            main: '#146a7d',
        },
        secondary: {
            main: '#20aac9',
        },
        divider: '#434b55',
    },
    typography: {
        fontFamily: '"Helvetica Now", sans-serif',
        fontSize: 14,
        h1: {
            fontSize: '1.8rem',
        },
        h2: {
            fontSize: '1.3rem',
        },
        body1: {
            fontSize: '0.95rem',
        },
        body2: {
            fontSize: '0.8rem',
        },
        caption: {
            fontSize: '0.7rem',
        },
    },
    components: {
        MuiContainer: {
            styleOverrides: {
                root: {
                    display: 'flex',
                    alignSelf: 'center',
                    flexDirection: 'column',
                    width: '90%',
                    height: '90vh',
                    margin: '1rem',
                },
            },
        },
        MuiPaper: {
            styleOverrides: {
                root: {
                    width: '90vw',
                    padding: '1rem 2rem',
                    margin: '1rem',
                    backgroundColor: 'var(--background-primary)',
                    border: '1px solid var(--border-primary) !important',
                    borderRadius: '20px',
                    flexDirection: 'column',
                },
            },
        },
        MuiTypography: {
            styleOverrides: {
                h2: {
                    margin: '0.5rem 0',
                    color: 'var(--text-color-primary)',
                    fontSize: 'var(--font-size-h2)',
                },
            },
        },
        MuiTableContainer: {
            styleOverrides: {
                root: {
                    height: '60vh',
                    width: '100%',
                    overflowY: 'auto',
                    overflowX: 'auto',
                },
            },
        },
        MuiTable: {
            styleOverrides: {
                root: {
                    width: '100%',
                    overflowX: 'hidden',
                    borderSpacing: 0,
                    borderCollapse: 'separate',
                },
            },
        },
        MuiTableHead: {
            styleOverrides: {
                root: {
                    position: 'sticky',
                    top: 0,
                    backgroundColor: 'var(--background-primary)',
                    textAlign: 'left',
                    textWrap: 'nowrap',
                },
            },
        },
        MuiTableCell: {
            styleOverrides: {
                head: {
                    padding: '0.5rem 1rem 0.5rem 0.4rem',
                    fontSize: 'var(--font-size-secondary-text)',
                    color: 'var(--text-color-primary)',
                    borderStyle: 'solid',
                    borderColor: 'var(--border-primary)',
                    borderWidth: '0.1rem 0',
                },
                body: {
                    whiteSpace: 'nowrap',
                    padding: '0.5rem 0.4rem',
                    fontSize: 'var(--font-size-tertiary-text)',
                    color: 'var(--text-color-secondary)',
                    borderStyle: 'solid',
                    borderColor: 'var(--border-secondary)',
                    borderWidth: '0 0 0.025rem',
                },
            },
        },
        MuiIconButton: {
            styleOverrides: {
                root: {
                    color: 'var(--text-color-links) !important',
                    '&:hover': {
                        color: 'var(--text-color-links-hover) !important',
                    },
                },
            },
        },
    },
});

export const RegistryTableTheme = ({ children }: { children: ReactNode }) => {
    const prefersDarkMode = useMediaQuery('(prefers-color-scheme: dark)');

    const theme = useMemo(() => (prefersDarkMode ? darkTheme : lightTheme), [prefersDarkMode]);


    return (
        <ThemeProvider theme={theme}>
            <CssBaseline />
            {children}
        </ThemeProvider>
    );
};